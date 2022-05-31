<script lang="ts">
import { defineComponent, computed } from "vue";
import useSignupStore from "../../stores/signup";
import { TgInput, TgTitle } from "../common";

export default defineComponent({
  name: "SignupForm",
  components: { TgInput, TgTitle },
  setup() {
    const store = useSignupStore();

    const email = computed({
      get(): string {
        return store.email;
      },
      set(email: string): void {
        store.email = email;
      },
    });

    const firstName = computed({
      get(): string {
        return store.firstName;
      },
      set(firstName: string): void {
        store.firstName = firstName;
      },
    });

    const lastName = computed({
      get(): string {
        return store.lastName;
      },
      set(lastName: string): void {
        store.lastName = lastName;
      },
    });

    const password = computed({
      get(): string {
        return store.password;
      },
      set(password: string): void {
        store.password = password;
      },
    });

    const passwordConfirm = computed({
      get(): string {
        return store.passwordConfirm;
      },
      set(password: string): void {
        store.passwordConfirm = password;
      },
    });

    const errors = computed(() => store.errors);

    const loading = computed(() => store.loading);

    async function registerUser(): Promise<any> {
      await store.signup();
    }

    return {
      email,
      firstName,
      lastName,
      password,
      passwordConfirm,
      loading,
      errors,
      registerUser,
    };
  },
});
</script>

<template>
  <div class="block login-page__signup" data-cy="signup">
    <TgTitle size="2" color="info-light" data-cy="signup__title">Sign up</TgTitle>
    <div class="block">
      <form @submit.prevent="registerUser">
        <TgInput
          v-model="firstName"
          name="first-name"
          :class-name="'tg-signup-form'"
          color="primary-dark"
          required
          placeholder="Jane"
          data-cy="signup__first-name"
          label="First name"
        />
        <TgInput
          v-model="lastName"
          name="last-name"
          :class-name="'tg-signup-form'"
          color="primary-dark"
          required
          placeholder="Doe"
          label="Last name"
          data-cy="signup__last-name"
        />
        <TgInput
          v-model="email"
          name="email"
          :class-name="'tg-signup-form'"
          color="primary-dark"
          input-type="email"
          required
          placeholder="email@address.com"
          :error-text="errors.email"
          data-cy="signup__email"
          label="Email"
        />
        <TgInput
          v-model="password"
          name="password"
          color="primary-dark"
          :class-name="'tg-signup-form'"
          input-type="password"
          required
          placeholder="************"
          label="Password"
          :error-text="errors.password"
          data-cy="signup__password"
        />
        <TgInput
          v-model="passwordConfirm"
          name="password-confirm"
          :class-name="'tg-signup-form'"
          color="primary-dark"
          input-type="password"
          required
          placeholder="************"
          label="Confirm password"
          :error-text="errors.passwordConfirm"
          data-cy="signup__password-confirm"
        />
        <div class="field">
          <div class="control">
            <button
              class="button is-success is-fullwidth"
              :class="{ 'is-loading': loading }"
              :disabled="loading"
              data-cy="signup__submit"
            >
              Sign up
            </button>
          </div>
        </div>
      </form>
    </div>
    <div class="block">
      <p class="has-text-info-light" data-cy="signup__login">
        Already have an account?
        <router-link :to="{ name: 'login' }" class="has-text-success login-page__signup"> Sign in </router-link>
      </p>
    </div>
  </div>
</template>

<style lang="scss" scoped>
@import "../../scss/fonts";
@import "../../scss/colors";
.tg-input {
  &:deep(.tg-input__label) {
    color: $white-ice;
  }
  &:deep(.input) {
    color: $white-ice;
    &:focus {
      border-color: $green-atomic;
      ~ .icon {
        color: $white-ice !important;
      }
    }
  }
  &:deep(.has-background-primary-dark) {
    border: 0.5px solid $grey-light;
    &:focus {
      color: $white-ice;
    }
    &::placeholder {
      color: $white-ice;
      opacity: 70%;
    }
  }
}
.button {
  font-family: $family-medium;
  font-weight: 800;
}
</style>
